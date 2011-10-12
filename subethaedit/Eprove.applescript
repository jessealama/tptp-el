tell application "SubEthaEdit"
     activate
     set Afile to (file of text window 1) as text
end tell

set unixname to POSIX path of Afile

set old_delimits to AppleScript's text item delimiters
set AppleScript's text item delimiters to "/"
-- pull the path out as a string and strip off the filename
set path_text to unixname
set path_list to (text items of path_text)
set final_path to items 1 thru ((count of path_list) - 1) of path_list
set infile to last item of path_list
set working_directory to (final_path as string)
set AppleScript's text item delimiters to old_delimits
tell application "Terminal"
     activate
     do script with command "cd " & "\"" & working_directory & "\";" & " eprover -l4 -R --print-statistics -xAuto -tAuto   --memory-limit=1000 --tptp3-format " & infile & " | epclextract"
end tell
