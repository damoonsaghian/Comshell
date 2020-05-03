/*
pause any playing file, then play the current file;
when done, using emacsclint, go to next file (in the project tree), while save-excursion, and then:
, if it's an audio file, play it;
, if it's a non_audio file, go to next file, try again;
, if it's a directory, my-find-file, go to the first line, then try agian;
*/
