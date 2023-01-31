from time import sleep
import os
from transformers import AutoModelForSequenceClassification
from transformers import AutoTokenizer
import pandas as pd
import numpy as np
import torch
from datasets import Dataset
from torch.utils.data import DataLoader
from tqdm import tqdm
from scipy.special import softmax

## PARAMETERS
file_to_save = "./data/predictions.csv"
file_to_save = "./data/predictions_modified.csv"
file_to_save = "./data/predictions_modified_new.csv"

models = os.listdir("models")
len(models)
models
assert(len(models) == 70)

result = pd.read_csv(file_to_save, header=None)
result
# result = result[0:4567876]
result.columns = ['model','a','b','c','d']
result.model.value_counts()
already_ran = result.model.unique().tolist()

models_to_run = [x for x in models if x not in already_ran]

result[result.model != 'mastery10-221107-0246'].to_csv()

# # check if we can open all models
# good_models = []
# for model_dir in models:
#   try:
#     model = AutoModelForSequenceClassification.from_pretrained("models/"+model_dir)
#     tokenizer = AutoTokenizer.from_pretrained("models/"+model_dir)
#     print("opened " + model_dir)
#   except:
#     print("cannot open "+ model_dir)
#   else:
#     good_models.append(model_dir)
# good_models

# # Read and save np list
# with open("output/good_models.txt", 'w') as f:
#     for s in good_models:
#         f.write(str(s) + '\n')

# with open("../good_models", 'r') as f:
#     good_models = [line.rstrip('\n') for line in f]

pd.read_parquet("all_essays_data_redacted.parquet")[['redacted']].to_parquet("redacted_essays.parquet")

data = pd.read_parquet("data/redacted_essays.parquet")[['redacted']]
data = pd.read_parquet("data/all_modified.parquet")[['redacted']]
data = pd.read_parquet("data/updated_all_modified.parquet")[['redacted']]

def get_predictions(model_dir, batch_size, text_col, file_name, data):
    device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
    model = AutoModelForSequenceClassification.from_pretrained("models/"+model_dir)
    model.to(device)
    tokenizer = AutoTokenizer.from_pretrained("models/"+model_dir)

    def tokenize_function(examples):
      return tokenizer(examples[text_col], padding="max_length", truncation=True)
    dataset = Dataset.from_pandas(data)
    dataset = Dataset.from_pandas(data)
    dataset = dataset.map(tokenize_function, batched=True)
    dataset = dataset.remove_columns(['redacted','__index_level_0__'])
    dataset.set_format("torch")

    dataloader = DataLoader(dataset, batch_size=batch_size)

    print("Generating predictions on the full dataset")
    progress_bar = tqdm(range(len(dataloader)))
    model.eval()
    with torch.no_grad():
      for batch in dataloader:
        batch = {k: v.to(device) for k, v in batch.items()}
        outputs = model(**batch)
        logits = outputs['logits'].cpu().detach().numpy()
        pps = softmax(logits, axis = 1)
        
        logits = pd.DataFrame(logits)
        pps = pd.DataFrame(pps)
        data = pd.concat([logits, pps], axis = 1)
        data = data.assign(model = model_dir)
        data.columns = ['l0','l1','p0','p1','model']
        data = data[['model','l0','l1','p0','p1']]
        data.to_csv(file_name,header = False, index = False, mode= "a")
        progress_bar.update(1)

# Run one of each model first

models.sort()
models[0:70:10]

for i,model in enumerate(models[0:70:10]):
  print(model)
  print(f"running model number {i+1} out of {len(models)}")
  get_predictions(model_dir = model, batch_size=300, text_col= "redacted",file_name=file_to_save, data=data)
  torch.cuda.empty_cache()
  
for i,model in enumerate(models):
  print(model)
  print(f"running model number {i+1} out of {len(models)}")
  get_predictions(model_dir = model, batch_size=300, text_col= "redacted",file_name=file_to_save, data=data)
  torch.cuda.empty_cache()

# for i,model in enumerate(models_to_run):
#   print(model)
#   print(f"running model number {i+1} out of {len(models_to_run)}")
#   get_predictions(model_dir = model, batch_size=300, text_col= "redacted",file_name="data/predictions.csv", data=data)
#   torch.cuda.empty_cache()
  
# # # Check to see whether it is still running
# from time import sleep
# import pandas as pd
# result = pd.read_csv("data/predictions.csv", header=None)
# print(f"total sample n: {result.shape[0]}\n"
#       f"total n models: {result.shape[0]/306463:.4f}\n"
#       f"total progress: {result.shape[0]*100/(306463*70):.4f}")

# for i in range(10):
#   result = pd.read_csv("data/predictions.csv", header=None)
#   print(f"total sample n: {result.shape[0]}\n"
#         f"total n models: {result.shape[0]/306463:.4f}\n"
#         f"total progress: {result.shape[0]*100/(306463*70):.4f}")
#   sleep(1)

# result.columns = ["model","l0","l1","p0","p1"]

# result = result[result.model != "selfconcordance9-221107-0217"]
# result.to_csv("data/predictions2.csv")

get_predictions(model_dir = "selfconcordance9-221107-0217", batch_size=300, text_col= "redacted",file_name="data/predictions2.csv", data=data)

result = pd.read_csv("data/predictions2.csv")
result

lastmodel = result[21452410-306463:21452410]
lastmodel.columns = ['model', 'l0', 'l1', 'p0', 'p1','drop']
lastmodel = lastmodel[['model', 'l0', 'l1', 'p0', 'p1']]

lastmodel
firstmodels = result[0:21452410-306463][['model', 'l0', 'l1', 'p0', 'p1']]

final_data = pd.concat([lastmodel,firstmodels],axis=0)
final_data.to_csv("data/predictions3.csv", index = False)

sdf = pd.read_csv('data/predictions3.csv')
final_data
sdf
sdf.reset_index(drop=True) == final_data.reset_index(drop=True)