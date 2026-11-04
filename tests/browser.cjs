const {chromium}=require(process.env.PLAYWRIGHT_MODULE||'playwright');
const fs=require('fs');
(async()=>{const browser=await chromium.launch({headless:true,channel:'msedge'});const page=await browser.newPage({viewport:{width:1440,height:1000},acceptDownloads:true});const errors=[];page.on('pageerror',e=>errors.push(e.message));await page.goto(process.env.LAB_URL||'http://127.0.0.1:8877');
const scope=process.env.LAB_URL?page.frameLocator('iframe').first():page;await scope.locator('.shiny-bound-input').first().waitFor({timeout:180000});async function tab(name){await scope.getByRole('tab',{name,exact:true}).click();await page.waitForTimeout(650);}async function noErrors(){const err=await scope.locator('.shiny-output-error').allTextContents();if(err.some(x=>x.trim()))throw Error(err.join('\n'));}
await tab('課本範例');await scope.locator('#book_result svg').first().waitFor();await noErrors();
for(const method of ['z','pooled','welch']){await scope.locator('#bm').selectOption(method);await page.waitForTimeout(250);await noErrors();}
for(const tail of ['left','right','two']){await scope.locator('#bt').selectOption(tail);await page.waitForTimeout(250);await noErrors();}
await page.screenshot({path:__dirname+'/../test-output/textbook.png',fullPage:true});
for(const example of ['age','paired','levene']){await scope.locator('#example').selectOption(example);await page.waitForTimeout(400);await noErrors();}
await tab('調查研究');
for(const group of ['Gender','iphone','unlimited','zodiac']){await scope.locator('#group').selectOption(group);await page.waitForTimeout(600);await noErrors();for(const variable of ['Age','B1','CT','B5']){await scope.locator('#variable').selectOption(variable);await page.waitForTimeout(180);await noErrors();}}
await page.screenshot({path:__dirname+'/../test-output/survey.png',fullPage:true});
await scope.locator('#g2').selectOption('水瓶座');await page.waitForTimeout(300);if(!(await scope.locator('#survey_info').textContent()).includes('不同群組'))throw Error('same-group guard');await scope.locator('#g2').selectOption('雙魚座');await page.waitForTimeout(300);
await tab('習作詳解');await scope.locator('#exercises details').first().waitFor();await noErrors();for(const d of await scope.locator('#exercises>details').all())await d.evaluate(el=>el.open=true);await page.screenshot({path:__dirname+'/../test-output/exercises.png',fullPage:true});
const [answer]=await Promise.all([page.waitForEvent('download'),scope.locator('#answers_download').click()]);await answer.saveAs(__dirname+'/../test-output/answers.html');if(!fs.readFileSync(__dirname+'/../test-output/answers.html','utf8').includes('習作三'))throw Error('answer export');
await tab('自行輸入');for(const method of ['z','pooled','welch','paired']){await scope.locator('#mm').selectOption(method);await page.waitForTimeout(300);await noErrors();}
await scope.locator('#n2').fill('9');await scope.locator('#n2').press('Tab');await page.waitForTimeout(350);if(!(await scope.locator('#manual_result').textContent()).includes('相同配對數'))throw Error('pair guard');await scope.locator('#n2').fill('8');await scope.locator('#n2').press('Tab');
await page.setViewportSize({width:390,height:844});await tab('課本範例');await scope.locator('#example').selectOption('uber');await page.waitForTimeout(500);await noErrors();console.log('mobile width',await page.evaluate(()=>({scroll:document.documentElement.scrollWidth,client:document.documentElement.clientWidth})));await page.screenshot({path:__dirname+'/../test-output/mobile.png',fullPage:true});
if(errors.length)throw Error(errors.join('\n'));console.log('PASS browser: methods, tails, textbook examples, four survey groupings, variables, guards, answer download, mobile.');await browser.close();})().catch(e=>{console.error(e);process.exit(1)});
