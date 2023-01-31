import shutil
from datasets import Dataset
import pandas as pd
from transformers import AutoTokenizer
from torch.utils.data import DataLoader
from transformers import AutoModelForSequenceClassification
from torch.optim import AdamW
from transformers import get_scheduler
import torch
import os
from tqdm.auto import tqdm
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import train_test_split
import numpy as np
import scipy as sp

# Ran this with four epochs and found out that 2 were optimal
def train_model(outcome_col = 'teamwork',
                text_col = "redacted",
                checkpoint = "LM Finetune Final",
                num_epochs = 4,
                fold = 1,
                batch_size = 16,
                pred_batch_size = 32,
                learning_rate = 5e-5,
                zip = False):
  def eval_model(dataloader,fold):
    model.eval()
    print("getting preds")
    progress_bar = tqdm(range(len(dataloader)))
    with torch.no_grad():
      for batch in dataloader:
        batch = {k: v.to(device) for k, v in batch.items()}
        outputs = model(**batch)
        logits = outputs['logits'].cpu().detach().numpy()
        pps = sp.special.softmax(logits, axis = 1)
        data = pd.DataFrame(pps[:,1]).assign(epoch = epoch, training_step = train_step, pps = pps[:,1], y = batch['labels'].cpu().detach().numpy(), fold = fold)
        data.to_csv(dir+"predictions.csv", header=None, index=False, mode = "a")
        progress_bar.update(1)

      print("getting metrics")
      data = pd.read_csv(dir + "predictions.csv", header=None) 
      data.columns = ['0', 'epoch','training_step','pps','y',"fold"]
      data = data[(data.epoch == epoch) & (data.training_step == train_step) & (data.fold == fold)]
      r, p = sp.stats.pearsonr(data['y'],data['pps'])
      auc = roc_auc_score(data['y'],data['pps'])
      
      print(f"{fold} AUC: {auc:.3f}")
      print(f"{fold} r  : {r:.3f}")
      
      pd.DataFrame.from_dict(
        {"model" : [dir],
        "epoch":[epoch],
        "training_step" : [train_step],
        "auc":[auc],
        "r":[r],
        "p":[p],
        'fold' : [fold]}
        ).to_csv(dir+"/model_stats.csv",mode = "a", index= False, header = None)
  
  print("Wrangling data")
  dir = "graduation_models/"
  if not os.path.exists(dir): os.mkdir(dir)
  
  all_essays_data = pd.read_parquet("data/all_essays_data_redacted.parquet")
  all_essays_data = all_essays_data[['id','redacted',outcome_col]].dropna()
  all_essays_data[outcome_col] = all_essays_data[outcome_col].astype(int)

  all_essays_data, drop = train_test_split(all_essays_data, train_size=18_000, random_state=463)
  train,test = train_test_split(all_essays_data, test_size=.3, random_state=235)
  test, val = train_test_split(test, test_size=.5, random_state=465)
  
  train.to_csv(dir+"/traindf.csv", index = False) 
  val.to_csv(dir+"/valdf.csv", index = False) 
  test.to_csv(dir+"/testdf.csv", index = False) 
  
  dataset_train = Dataset.from_pandas(pd.read_csv(dir+"/traindf.csv").dropna())
  dataset_val = Dataset.from_pandas(pd.read_csv(dir+"/valdf.csv").dropna())
  dataset_test = Dataset.from_pandas(pd.read_csv(dir+"/testdf.csv").dropna())

  # Tokenize dataset
  print("Tokenizing dataset")
  tokenizer = AutoTokenizer.from_pretrained(checkpoint)
  def tokenize_function(examples):
    return tokenizer(examples[text_col], padding="max_length", truncation=True)

  def format_dataset(dataset_train):        
    dataset_train = dataset_train.remove_columns([x for x in dataset_train.column_names if x not in [text_col,outcome_col]])
    dataset_train = dataset_train.map(tokenize_function, batched=True)
    dataset_train = dataset_train.remove_columns([x for x in dataset_train.column_names if x not in [outcome_col,"input_ids","type_ids",'attention_mask']])
    dataset_train = dataset_train.rename_column(outcome_col, "labels")
    dataset_train.set_format("torch")
    return(dataset_train)
  
  dataset_train = format_dataset(dataset_train)
  dataset_val = format_dataset(dataset_val)
  dataset_test = format_dataset(dataset_test)
  
  train_dataloader = DataLoader(dataset_train, shuffle=False, batch_size=batch_size)
  val_dataloader = DataLoader(dataset_val, shuffle = False, batch_size=pred_batch_size)
  test_dataloader = DataLoader(dataset_test, batch_size=pred_batch_size, shuffle=False)

  # Model specs
  print("Downloading and configuring model")
  model = AutoModelForSequenceClassification.from_pretrained(checkpoint, num_labels=2)
  optimizer = AdamW(model.parameters(), lr=learning_rate)
  num_training_steps = num_epochs * len(train_dataloader)
  lr_scheduler = get_scheduler(name="linear", optimizer=optimizer, num_warmup_steps=0, num_training_steps=num_training_steps)
  device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
  model.to(device)
  
  # Model training
  print("Training the model")
  train_step = 0
  progress_bar = tqdm(range(num_training_steps))
  model.train()
  for epoch in range(num_epochs):
    print("Beggining training on epoch "+str(epoch))
    for batch in train_dataloader:
      batch = {k: v.to(device) for k, v in batch.items()}
      outputs = model(**batch)
      loss = outputs.loss
      with open(dir+"loss.txt","w") as f:
        f.write(f"{outputs.loss.item()}\n")
      loss.backward()
      optimizer.step()
      lr_scheduler.step()
      optimizer.zero_grad()
      progress_bar.update(1)
      train_step += batch_size
        
    if train_step % (600) == 0:
      model.eval()
      eval_model(val_dataloader, "val")
      model.train()
    
    model.eval()
    eval_model(train_dataloader, "train")
    
    model.eval()
    eval_model(val_dataloader, "val")

  model.save_pretrained(dir)
  tokenizer.save_pretrained(dir)
  if zip:
    shutil.make_archive(dir, 'zip', dir)

train_model(outcome_col = "grad6", fold = 1, checkpoint='roberta-base',learning_rate=3e-5, batch_size= 16, pred_batch_size=100, num_epochs=2)

# Evaluate four epochs to determine how many epochs to run
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
ms = pd.read_csv("graduation_models_test/model_stats.csv")
ms = ms[ms.model != "model"]
ms.auc = ms.auc.astype(float)
ms.epoch = ms.epoch.astype(float)
ms
sns.lineplot(data = ms, x = "epoch", y = "auc", hue = "fold")
plt.show()
plt.savefig('save_as_a_png.png')

# Get predictions in the test set
dir = "graduation_models"
dataset_test = Dataset.from_pandas(pd.read_csv(dir+"/testdf.csv").dropna())
print("Tokenizing dataset")
tokenizer = AutoTokenizer.from_pretrained('graduation_models')
def tokenize_function(examples):
  return tokenizer(examples['redacted'], padding="max_length", truncation=True)
def format_dataset(dataset_train):        
  dataset_train = dataset_train.remove_columns([x for x in dataset_train.column_names if x not in ['redacted','grad6']])
  dataset_train = dataset_train.map(tokenize_function, batched=True)
  dataset_train = dataset_train.remove_columns([x for x in dataset_train.column_names if x not in ['grad6',"input_ids","type_ids",'attention_mask']])
  dataset_train = dataset_train.rename_column('grad6', "labels")
  dataset_train.set_format("torch")
  return(dataset_train)
dataset_test = format_dataset(dataset_test)
test_dataloader = DataLoader(dataset_test, batch_size=100, shuffle=False)       
def eval_model(dataloader,fold, checkpoint = "graduation_models", device = "cuda", epoch = 1, train_step = None, dir = "graduation_models"):
    model = AutoModelForSequenceClassification.from_pretrained(checkpoint)
    model.to(device)
    model.eval()
    print("getting preds")
    progress_bar = tqdm(range(len(dataloader)))
    with torch.no_grad():
      for batch in dataloader:
        batch = {k: v.to(device) for k, v in batch.items()}
        outputs = model(**batch)
        logits = outputs['logits'].cpu().detach().numpy()
        pps = sp.special.softmax(logits, axis = 1)
        data = pd.DataFrame(pps[:,1]).assign(epoch = epoch, training_step = train_step, pps = pps[:,1], y = batch['labels'].cpu().detach().numpy(), fold = fold)
        data.to_csv(dir+"predictions_test.csv", header=None, index=False, mode = "a")
        progress_bar.update(1)

      print("getting metrics")
      data = pd.read_csv(dir + "predictions_test.csv", header=None) 
      data.columns = ['0', 'epoch','training_step','pps','y',"fold"]
      data = data[(data.epoch == epoch) & (data.training_step == train_step) & (data.fold == fold)]
      r, p = sp.stats.pearsonr(data['y'],data['pps'])
      auc = roc_auc_score(data['y'],data['pps'])
      
      print(f"{fold} AUC: {auc:.3f}")
      print(f"{fold} r  : {r:.3f}")
      
      pd.DataFrame.from_dict(
        {"model" : [dir],
        "epoch":[epoch],
        "training_step" : [train_step],
        "auc":[auc],
        "r":[r],
        "p":[p],
        'fold' : [fold]}
        ).to_csv(dir+"/model_stats.csv",mode = "a", index= False, header = None)
eval_model(test_dataloader, fold = "test")

# Interpret
from transformers_interpret import SequenceClassificationExplainer
import torch
import pandas as pd
from transformers import AutoModelForSequenceClassification
from transformers import AutoTokenizer
import os

data = pd.read_csv("graduation_models/traindf.csv")
data.redacted

def explain(model_dir, series): 
  print("preparing")
  device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
  model = AutoModelForSequenceClassification.from_pretrained(model_dir)
  model.to(device)
  tokenizer = AutoTokenizer.from_pretrained(model_dir)
  cls_explainer = SequenceClassificationExplainer(model,tokenizer)
  for i,j in enumerate(series):
    print(f"explaining: {i/len(series)*100:.2f}% done", end="\r")
    word_attributions = cls_explainer(j, class_name = "LABEL_1")
    cls_explainer.visualize("graduation_models/explain/"+str(i)+".html")
    pd.DataFrame(word_attributions).assign(id = i).assign(model = model_dir).to_csv("output/word_attributions_direct.csv", mode="a", index=False, header=False)

explain("graduation_models",data.redacted[0:4000])


