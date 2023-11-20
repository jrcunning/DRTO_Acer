
String getdate()
{
   DateTime t = rtc.now();
   String dateStr =  String(t.year(), DEC) + "_" + String(t.month(),DEC)  + "_" + String(t.day(), DEC);
   return dateStr;
}


void checkTime()
{
  t = rtc.now();
  /*Serial.print("Time:\t");
  //Serial.print(String(t.hour, DEC));
  Serial.print(":");
  Serial.println(t.minute, DEC);
  Serial.print("Date:\t"+String(t.month, DEC)+ "/" +String(t.day, DEC)+ "/" +String(t.year, DEC)+"\t" +t.dayOfTheWeek);
*/
  
  Serial.println("Date:\t"+t.timestamp(DateTime::TIMESTAMP_DATE));
  Serial.println("Time:\t"+t.timestamp(DateTime::TIMESTAMP_TIME));
}

void lcdTimeDisplay()
{
  DateTime t=rtc.now();
  lcd.clear();
  
  lcd.setCursor(3, 0);
  lcd.print(t.timestamp(DateTime::TIMESTAMP_TIME));

  lcd.setCursor(0, 1);
  // Display abbreviated Day-of-Week in the lower left corner
  lcd.print(t.dayOfTheWeek());

  lcd.setCursor(4,1);
  lcd.print(t.timestamp(DateTime::TIMESTAMP_DATE));
  
}


void checkRTC(){
  if (! rtc.begin()) {
   Serial.println("RTC NOT Found :(");   
 }
  if (rtc.begin()) {
   Serial.println("RTC Found!");   
 }
}
