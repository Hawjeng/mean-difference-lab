function saveLocalFile(content,name,mime){const blob=new Blob([content],{type:mime});const url=URL.createObjectURL(blob);const a=document.createElement('a');a.href=url;a.download=name;document.body.appendChild(a);a.click();a.remove();setTimeout(()=>URL.revokeObjectURL(url),30000);}
function downloadCurrentSVG(){const svg=document.querySelector('#survey_chart svg');if(!svg){alert('請先等待圖形完成。');return;}const clone=svg.cloneNode(true);clone.setAttribute('xmlns','http://www.w3.org/2000/svg');clone.setAttribute('width','960');clone.setAttribute('height',svg.viewBox.baseVal.height);const name=(document.querySelector('#variable')?.value||'zodiac')+'-CI.svg';saveLocalFile('<?xml version="1.0" encoding="UTF-8"?>\n'+new XMLSerializer().serializeToString(clone),name,'image/svg+xml;charset=utf-8');}
$(document).on('shiny:connected',function(){Shiny.addCustomMessageHandler('download-file',function(data){saveLocalFile(data.content,data.name,data.mime);});});
document.addEventListener('click',function(event){
 const button=event.target.closest('.lab-code-download');
 if(button)saveLocalFile(button.parentElement.querySelector('textarea[aria-label^="'+(button.dataset.filename.endsWith('.R')?'R':'Python')+'"]').value,button.dataset.filename,'text/plain;charset=utf-8');
});
document.addEventListener('click', event => {
 const button=event.target.closest('.lab-python-run');
 if(!button)return;
 const block=button.closest('.syntax-block'), output=block.querySelector('.python-console');
 const code=block.querySelector('textarea').value;
 button.disabled=true;
 output.textContent='正在載入 Python 並執行…';
 let worker,timer;
 const finish=(message,ok)=>{
  clearTimeout(timer);
  worker?.terminate();
  output.textContent=ok?message:message+'\n請重新整理或換用新版瀏覽器及網路。也可下載 .py，在本機先執行 python -m pip install scipy，再執行程式。';
  button.disabled=false;
 };
 try{
  worker=new Worker('python-console.js',{type:'module'});
  worker.onmessage=({data})=>finish(data.output,data.ok);
  worker.onerror=()=>finish('無法載入 Python 執行環境。',false);
  timer=setTimeout(()=>finish('Python 執行逾時。',false),120000);
  worker.postMessage({code});
 }catch(error){finish(error.message,false);}
});
