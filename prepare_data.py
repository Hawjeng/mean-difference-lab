"""Read private workbooks locally; publish only group summaries and textbook data."""
from pathlib import Path
import json,statistics as st,math,re,hashlib,csv,shutil,os
import openpyxl
root=Path(__file__).parent
survey=Path(os.environ['SURVEY_XLSX'])
w=openpyxl.load_workbook(survey,data_only=True); rs=list(w['Cleaned_Data'].values);records=[dict(zip(rs[0],r)) for r in rs[1:]]
variables={'Age':('年齡','歲'),'CT':('手機遊戲成癮','分'),'A1':('Facebook 好友數','人'),'A2':('LINE 好友數','人'),'A3':('LINE 群組數','個'),'A4':('Instagram 粉絲數','人'),'A5':('Instagram 追蹤數','人'),'B1':('玩手遊時間','原問卷時間單位'),'B2':('觀看影片時間','原問卷時間單位'),'B3':('外送訂餐次數','次'),'B4':('搭乘計程車次數','次'),'B5':('電量焦慮','原問卷量尺'),'BirthYear':('出生年','年')}
zodiac=['水瓶座','雙魚座','牡羊座','金牛座','雙子座','巨蟹座','獅子座','處女座','天秤座','天蠍座','射手座','摩羯座']
def code(v):
 m=re.match(r'^(\d+)',str(v or ''));return int(m[1]) if m else None
for r in records:
 c=code(r['PhoneBrand']);r['iphone']='iPhone' if c==1 else '非 iPhone' if c in range(2,11) else None
 c=code(r['DataPlan']);r['unlimited']='吃到飽' if c==1 else '非吃到飽' if c in [2,3,4] else None
 r['zodiac']=zodiac[int(r['Z'])-1] if r['Z'] in range(1,13) else None
def summary(a,total):
 if not a:return {'n':0,'total':total,'missing':total}
 n=len(a);mean=st.mean(a);median=st.median(a);q=st.quantiles(a,n=4,method='inclusive') if n>1 else [a[0]]*3
 out={'n':n,'total':total,'missing':total-n,'mean':mean,'sd':st.stdev(a) if n>1 else 0,'min':min(a),'q1':q[0],'median':median,'q3':q[2],'max':max(a)}
 for center,label in [(mean,'meanAbs'),(median,'medianAbs')]:
  d=[abs(x-center) for x in a];out[label]={'mean':st.mean(d),'sd':st.stdev(d) if n>1 else 0}
 return out
groups={'zodiac':{'label':'任選兩個星座','categories':zodiac},'Gender':{'label':'性別差異','categories':['女','男']},'iphone':{'label':'是否為 iPhone','categories':['iPhone','非 iPhone']},'unlimited':{'label':'是否為吃到飽','categories':['吃到飽','非吃到飽']}}
out={'source':'data2.xlsx / Cleaned_Data（C/Ch08，2026-11-05 提供）','sha256':hashlib.sha256(survey.read_bytes()).hexdigest(),'rowCount':len(records),'groups':groups,'variables':{}}
flat=[]
for v,(label,unit) in variables.items():
 def valid(r):return isinstance(r[v],(int,float)) and math.isfinite(r[v])
 a=[r[v] for r in records if valid(r)];obj={'label':label,'unit':unit,'overall':summary(a,len(records)),'groups':{}}
 for key,g in groups.items():
  obj['groups'][key]={}
  for cat in g['categories']:
   subset=[r for r in records if r[key]==cat];s=summary([r[v] for r in subset if valid(r)],len(subset));obj['groups'][key][cat]=s
   flat.append({'variable':v,'label':label,'grouping':key,'group':cat,**{k:s[k] for k in ['n','total','missing','mean','sd','min','q1','median','q3','max']}})
 out['variables'][v]=obj
(root/'app/data/survey.json').write_text(json.dumps(out,ensure_ascii=False,indent=2),encoding='utf-8')
with (root/'downloads/survey-summary.csv').open('w',encoding='utf-8-sig',newline='') as f:
 writer=csv.DictWriter(f,fieldnames=list(flat[0]));writer.writeheader();writer.writerows(flat)
base=Path(os.environ['CHAPTER_DATA_DIR']);names=['Ch10p274exercise10.3.xlsx','Ch10p266t10.7.xlsx','Ch10p248ex10.1-10.4.xlsx','Ch10p245age.xlsx']
books={};audit=[]
for name in names:
 file=base/name;sh=next(iter(openpyxl.load_workbook(file,data_only=True)));books[name]=list(sh.values)
 shutil.copy2(file,root/'downloads'/name);audit.append({'file':name,'sha256':hashlib.sha256(file.read_bytes()).hexdigest(),'sheet':sh.title})
uber=books[names[2]];age=books[names[3]];ex=books[names[0]]
textbook={'uber':{'a':[r[0] for r in uber[1:9]],'b':[r[1] for r in uber[1:9]]},'age':{'a':[r[1] for r in age[1:26]],'b':[r[2] for r in age[1:26]]},'papers':{'a':[r[1] for r in ex[1:9]],'b':[r[2] for r in ex[1:9]]},'audit':audit}
(root/'app/data/textbook.json').write_text(json.dumps(textbook,ensure_ascii=False,indent=2),encoding='utf-8')
(root/'downloads/data-audit.json').write_text(json.dumps({'survey':{'file':'data2.xlsx','sheet':'Cleaned_Data','sha256':out['sha256'],'rows':len(records)},'textbook':audit},ensure_ascii=False,indent=2),encoding='utf-8')
print('Prepared',len(records),'survey records as summaries only;',len(variables),'numeric variables; 4 workbook examples.')
