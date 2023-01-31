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
from datetime import datetime
import numpy as np
import scipy as sp

def train_model(outcome_col = 'teamwork',
                text_col = "response",
                checkpoint = "LM Finetune Final",
                num_epochs = 4,
                fold = 1,
                batch_size = 16,
                pred_batch_size = 32,
                learning_rate = 5e-5,
                zip = False):
  print("Wrangling data")
  dir = "../"+outcome_col+str(fold)+"-"+(datetime.now().strftime("%y%m%d-%H%M"))
  if not os.path.exists(dir): os.mkdir(dir)
  all_essays_data = pd.read_parquet("development.parquet")
  all_essays_data = all_essays_data[['id','response',outcome_col,"fold"]].dropna()
  all_essays_data[outcome_col] = all_essays_data[outcome_col].astype(int)
  all_essays_data['fold'] = all_essays_data['fold'].astype(int)

  train = all_essays_data[all_essays_data['fold'] != fold]
  train.to_csv(dir+"/traindf.csv") 
  all_essays_data.to_csv(dir+"/all.csv") 
  dataset_train = Dataset.from_pandas(pd.read_csv(dir+"/traindf.csv").dropna())
  dataset_all = Dataset.from_pandas(pd.read_csv(dir+"/all.csv").dropna())

  # Tokenize dataset
  print("Tokenizing dataset")
  tokenizer = AutoTokenizer.from_pretrained(checkpoint)
  def tokenize_function(examples):
    return tokenizer(examples[text_col], padding="max_length", truncation=True)

  dataset_train = dataset_train.remove_columns([x for x in dataset_train.column_names if x not in [text_col,outcome_col]])
  dataset_train = dataset_train.map(tokenize_function, batched=True)
  dataset_train = dataset_train.remove_columns([x for x in dataset_train.column_names if x not in [outcome_col,"input_ids","type_ids",'attention_mask']])
  dataset_train = dataset_train.rename_column(outcome_col, "labels")
  dataset_train.set_format("torch")

  dataset_all = dataset_all.remove_columns([x for x in dataset_all.column_names if x not in [text_col,outcome_col]])
  dataset_all = dataset_all.map(tokenize_function, batched=True)
  dataset_all = dataset_all.remove_columns([x for x in dataset_all.column_names if x not in [outcome_col,"input_ids","type_ids",'attention_mask']])
  dataset_all = dataset_all.rename_column(outcome_col, "labels")
  dataset_all.set_format("torch")

  train_dataloader = DataLoader(dataset_train, shuffle=False, batch_size=batch_size)
  full_dataloader = DataLoader(dataset_all, batch_size=pred_batch_size)

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
  progress_bar = tqdm(range(num_training_steps))
  f = open(dir+"/loss.txt", "w")
  model.train()
  for epoch in range(num_epochs):
    print("Beggining training on epoch "+str(epoch))
    for batch in train_dataloader:
          batch = {k: v.to(device) for k, v in batch.items()}
          outputs = model(**batch)
          loss = outputs.loss
          f.write(f"{outputs.loss.item()}\n")
          loss.backward()
          optimizer.step()
          lr_scheduler.step()
          optimizer.zero_grad()
          progress_bar.update(1)
  f.close()

  print("Generating predictions on the full dataset")
  model.eval()
  progress_bar = tqdm(range(len(full_dataloader)))
  with torch.no_grad():
    for batch in full_dataloader:
      batch = {k: v.to(device) for k, v in batch.items()}
      outputs = model(**batch)
      logits = outputs['logits'].cpu().detach().numpy()
      pps = sp.special.softmax(logits, axis = 1)
      with open(dir+"/pps.csv", "ab") as f:
        np.savetxt(f, pps)
        f.write(b"\n")

      with open(dir+"/logits.csv", "ab") as f:
        np.savetxt(f, logits)
        f.write(b"\n")
      progress_bar.update(1)
  
  print("Calculating metrics")
  t = pd.read_csv(dir+"/all.csv")
  p = pd.read_csv(dir+"/pps.csv",header=None, sep = " ")
  p.columns = ['p0', 'p1']
  all = pd.concat([t, p], axis=1)
  traindf = all[all.fold != fold]
  testdf = all[all.fold == fold]

  auc_train = roc_auc_score(traindf[outcome_col],traindf['p1'])
  r_train,p_train = sp.stats.pearsonr(traindf[outcome_col],traindf['p1'])

  auc_test = roc_auc_score(testdf[outcome_col],testdf['p1'])
  r_test,p_test = sp.stats.pearsonr(testdf[outcome_col],testdf['p1'])

  print("final model AUC (training):"+str(auc_train.round(4)))
  print("final model AUC (test):"+str(auc_test.round(4)))
  print("final model r (training):"+str(r_train.round(4)))
  print("final model r (test):"+str(r_test.round(4)))

  pd.DataFrame.from_dict({"model" : [dir],
                          "auc_train":[auc_train],
                          "r_train":[r_train],
                          "p_train":[p_train],
                          "auc_test":[auc_test],
                          "r_test":[r_test],
                          "p_test":[p_test]}).to_csv(dir+"/model_stats.csv")

  model.save_pretrained(dir)
  tokenizer.save_pretrained(dir)
  if zip:
    shutil.make_archive(dir, 'zip', dir)


# Use checkpoint = roberta-base to not use pretraining.
# for fold in range(10):
#   print(fold+1)
#   for outcome in ['prosocial', 'leadership', 'teamwork', 'mastery',
#        'perseverance', 'selfconcordance', 'goal']:
#        print(outcome)
#        train_model(outcome, fold = fold+1, checkpoint='checkpoint-6085-epoch-1',
#                    learning_rate=3e-5, batch_size= 16, pred_batch_size=128)

# for outcome in ['mastery',
#        'perseverance', 'selfconcordance', 'goal']:
#        print(outcome)
#        train_model(outcome, fold = 6, checkpoint='checkpoint-6085-epoch-1',
#                    learning_rate=3e-5, batch_size= 16, pred_batch_size=128)

# Use checkpoint = roberta-base to not use pretraining.
for fold in range(4):
  print(fold+7)
  for outcome in ['prosocial', 'leadership', 'teamwork', 'mastery',
       'perseverance', 'selfconcordance', 'goal']:
       print(outcome)
       train_model(outcome, fold = fold+7, checkpoint='checkpoint-6085-epoch-1',
                   learning_rate=3e-5, batch_size= 16, pred_batch_size=100)
       
for outcome in ['perseverance','selfconcordance', 'goal']:
  print(outcome)
  train_model(outcome, fold = 10, checkpoint='checkpoint-6085-epoch-1',
            learning_rate=3e-5, batch_size= 16, pred_batch_size=100)


train_model(outcome_col = "leadership", fold = 4, checkpoint='checkpoint-6085-epoch-1',
            learning_rate=3e-5, batch_size= 16, pred_batch_size=100)

