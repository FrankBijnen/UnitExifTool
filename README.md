# UnitExifTool
 Unit to use ExifTool in Delphi applications

# Features
 This unit takes advantage of the stay_open feature.
 Commands are executed asynchronous.
 Output of the ExifTool is provided by an OnReady event.

# Usage
 ##See the sample project.

 ##Include the unit UnitExifTool in your uses list.

 ##Create an instance of TExifTool, or optionally a specialized class.

 ** ExifTool := TExifTool.create; **

 Create has a optional parameter **ACmdLine: string** allowing a different command line to start ExifTool.
 The default is: **exiftool -stay_open true -@ -**  

 ##Write an Onready Event, and connect that to the ExiftTool.

 ** procedure TForm1.ExifToolReady(Sender: TObject; Content: TContent); **
 ** ExifTool.OnReady := ExifToolReady; **

 Content is a record, with the following info.

   TContent = record
     ExecNum: integer;
     Request: string;
     Response: string;
     Error: string;
   end;
 
 ##Send commands. 

 **  ExifTool.Send('c:\sample\sample.jpg' + #13 + #10); ** 

 Sending only the filename will return already a lot of info.
 Dont forget to add CR + LF after each line.

 In production applications it is probably better to load your commands in a stringlist and send that.

 Cmds := TStringList.create;
 try
   Cmds.LoadFromFile('cmds.txt'); // Refer to the ExifTool documentation
   ExifTool.Send(Cmds);
 finally
   Cmds.Free;
 end;

 ##CleanUp.

 ** ExifTool.Free **

 The Free ensures that ExifTool terminates.

 ## Build the exutable.

 ## Make sure the program finds the ExifTool executable. See the create method.

# The source, and sample, can be build with Delphi Community Edition. Currently 11.3

# Tested on Windows 7, 8 and 10. 64 Bits, with ExifTool version 12.63
