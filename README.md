# UnitExifTool
 Unit to use ExifTool in Delphi applications

# Features
 This unit takes advantage of the stay_open feature. <br>
 Commands are executed asynchronous. <br>
 Output of the ExifTool is provided by an OnReady event. <br>

# Usage
 ## See the sample project.

 ## Include the unit UnitExifTool in your uses list.
 >uses UnitExifTool;<br>
 
 ## Create an instance of TExifTool, or optionally a specialized class.

 >ExifTool := TExifTool.create;<br>

 Create has a optional parameter **ACmdLine: string** allowing a different command line to start ExifTool. <br>
 The default is: **exiftool -stay_open true -@ -**  <br>

 ## Write an Onready Event, and connect that to the ExiftTool. 

 >procedure TForm1.ExifToolReady(Sender: TObject; Content: TContent);<br>
 >ExifTool.OnReady := ExifToolReady;<br>

 Content is a record, with the following info. <br>
 
 >  TContent = record <br>
 >    ExecNum: integer; <br>
 >    Request: string; <br>
 >    Response: string; <br>
 >    Error: string; <br>
 >  end; <br>
 
 ## Send commands. 

 >ExifTool.Send('c:\sample\sample.jpg' + #13 + #10);<br>

 Sending only the filename will return already a lot of info. <br>
 Dont forget to add CR + LF after each line. <br>

 In production applications it is probably better to load your commands in a stringlist and send that. <br>

 >Cmds := TStringList.create; <br>
 >try <br>
 >  Cmds.LoadFromFile('cmds.txt'); // Refer to the ExifTool documentation <br>
 >  ExifTool.Send(Cmds); <br>
 >finally <br>
 >  Cmds.Free; <br>
 >end; <br>

 ## CleanUp.

 >ExifTool.Free;<br>

 The Free ensures that ExifTool terminates. <br>

 ## Build the executable.

 ## Make sure the program finds the ExifTool executable. See the create method.
<br>

# Compiler info
The source, and sample, can be build with Delphi Community Edition. Currently 11.3.<br>

# Tested on 
Windows 7, 8 and 10. 64 Bits, with ExifTool version 12.63.<br>
