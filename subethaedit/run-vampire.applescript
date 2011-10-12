-- This script uses run-vampire.sh; it assumes that this script is under the bin subdirectory of your home directory.

-- see settings
on seescriptsettings()
	
	return {displayName:"Run Vampire", shortDisplayName:"Vampire", toolbarIcon:"ToolbarIconRunInTerminal", inDefaultToolbar:"yes", toolbarTooltip:"Run Vampire on the current file", inContextMenu:"yes"}
	
	-- Here is a overview of currently supported settings
	-- displayName: Name displayed in menus
	-- shortDisplayName: The short display name used for Toolbar
	-- keyboardShortcut: A keyboard shortcut. alt=~, ctrl=^, shift=$, cmd=@
	-- toolbarIcon: A toolbar image
	-- inDefaultToolbar: Should be in default toolbar?
	-- toolbarTooltip: Tooltip for the toolbar icon
	-- inContextMenu: Should it appear in the ctrl-click menu?
	
end seescriptsettings

-- sanity check: the run-vampire.sh script exists and is executable
tell application "Finder"
	if not (exists POSIX file "~/bin/run-vampire.sh") then
		error "The run-vampire.sh script does not exist in the expect location (~/bin/run-vampire.sh)"
	end if
end tell


-- the actual script

tell application "SubEthaEdit"
	if exists path of front document then
		try
			save front document
		end try
	else
		error "You have to save the document first"
	end if
end tell

tell application "SubEthaEdit"
	set savedClipboard to the clipboard
	if (length of selection = 0) then
		set the clipboard to contents of front document as text
	else
		set the clipboard to contents of selection as text
	end if
end tell


-- the following is a command to call an external script, in this case perl
-- notice the export preamble which is essential to make pbpaste work with
-- utf8 content.
set shellscriptString to "pbpaste | vampire -t 300"
tell application "SubEthaEdit"
	set tptpPath to (path of front document)
end tell

set shellresult to do shell script ("~/bin/run-vampire.sh " & tptpPath) without altering line endings

tell application "SubEthaEdit"
	set the clipboard to savedClipboard
	set contents of selection of front document to shellresult
end tell