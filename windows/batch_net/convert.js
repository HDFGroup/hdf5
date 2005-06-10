/* Provided by: Microsoft
   URL: http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vccore/html/vclrfupgradingvisualcprojectstovisualstudionetinbatchmode.asp
*/
var vcProj = new ActiveXObject("VisualStudio.VCProjectEngine.7.1");
var objFile = new ActiveXObject("Scripting.FileSystemObject");
var objArgs = WScript.Arguments;

// check the arguments to be sure it's right
if (objArgs.Count() < 2)
{
   WScript.Echo("VC6 or 5 DSP Project File Conversion");
   WScript.Echo("Opens specified .dsp and converts to VC7.1 Format.");
   WScript.Echo("Will create project file with .vcproj extension");
   WScript.Echo("\n\tusage: <full path\project.dsp> <full path\project.vcproj>");
   WScript.Quit(1);
}

WScript.Echo("\nConverting: "+ objArgs.Item(0));
// If there is a file name of the .vcproj extension, do not convert
var vcProject = vcProj.LoadProject(objArgs.Item(0));
if (!objFile.FileExists(vcProject.ProjectFile))
{
   // specify name and location of new project file
   vcProject.ProjectFile = objArgs.Item(1);

   // call the project engine to save this off. 
   // when no name is shown, it will create one with the .vcproj name
   vcProject.Save();
   WScript.Echo("New Project Name: "+vcProject.ProjectFile+"\n");
}
else
{
   WScript.Echo("ERROR!: "+vcProject.ProjectFile+" already exists!\n");
}
