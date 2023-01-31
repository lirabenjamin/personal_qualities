from transformers_interpret import SequenceClassificationExplainer
import torch
import pandas as pd
from transformers import AutoModelForSequenceClassification
from transformers import AutoTokenizer
import os

# List models
models = os.listdir("models")
len(models)
models
assert(len(models) == 70)
models.sort()
models_to_explain = models[0:70:10]

result = pd.read_csv("./output/word_attributions.csv", index_col=0)
result
result.columns = ['word','score','id','model']
result.model.value_counts()

# Remove mastery1 and overwrite and restart
#result[result.model != 'mastery1-221106-1949'].to_csv("./output/word_attributions.csv", index = False)

already_ran = result.model.unique().tolist()
models_to_run = [x for x in models_to_explain if x not in already_ran]

# Get data
data = pd.read_parquet("data/development.parquet")[['response']]

def explain(model_dir): 
  print("preparing")
  device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu")
  model = AutoModelForSequenceClassification.from_pretrained("models/"+model_dir)
  model.to(device)
  tokenizer = AutoTokenizer.from_pretrained("models/"+model_dir)
  cls_explainer = SequenceClassificationExplainer(model,tokenizer)
  for i,j in enumerate(data.response):
    print(f"explaining: {i/3131*100:.2f}% done", end="\r")
    word_attributions = cls_explainer(j, class_name = "LABEL_1")
    pd.DataFrame(word_attributions).assign(id = i).assign(model = model_dir).to_csv("output/word_attributions.csv", mode="a", index=False, header=False)
    
for i, model in enumerate(models_to_run):
  print(f"model: {model}, {i+1} of {len(models_to_run)+1}")
  explain(model)
 

new_result = result[result.id == "mastery1-221106-1949"].reset_index()
new_result = new_result[['index', 'word','score','id']]
new_result.columns = ['word','score','id', 'model']

result = pd.concat([result,new_result])

result.model.value_counts()

value_counts = result.drop_duplicates()

value_counts.to_csv("output/word_attributions.csv")