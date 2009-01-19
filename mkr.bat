jar cvfM gamep.jar *.class *.pdr pdr/*.class game/*.class beep/*.class
java -jar proguard.jar @game.txt
copy /Y game.html bin\.
copy /Y game.jar bin\.
call app