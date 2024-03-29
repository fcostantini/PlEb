# PlEb  
A simple tool to manage playlists. Allows adding/removing songs (including full directories), format conversion, combining and exporting. Supported formats are m3u, m3u8, pls, wpl and xspf.  

## Requirements  

A modern version of stack. For Windows, Haskell Platform has everything you need. Alternatively an executable is available [here](https://github.com/fcostantini/PlEb/releases).  

## Installation  

    $ git clone https://github.com/fcostantini/PlEb.git  
    $ cd PlEb  
    $ stack setup  
    $ stack install  

It's recommended to add ~/.local/bin to your path for easier usage. You can also change the instalation directory with --local-bin-path dir.  

## Usage  

To load a playlist and use PlEb interactively:  

    $ PlEb playlist-file  

As an alternative, it is possible to feed a file with operations to PlEb, which will try to execute them:  

    $ PlEb playlist-file -c command-file  

This file must have a single line with the desired commands, i.e., when providing more than one, they must be chained (see below).  

### Operations  

    - add: adds a song to the playlist.  
    - add_dir: adds a directory to the playlist.  
    - check: checks if the songs actually exist.  
    - combine: combines playlists.  
    - convert: converts the playlist to another format.  
    - export: copies every (correct) song to a single folder.  
    - load: loads another playlist.  
    - print: shows contents of the playlist.  
    - rmv: removes a song from the playlist.  
    - exit/quit: exits the program.  

Commands can be chained using '>'. For example, we can do the following:  

    print>add song.mp3>print>quit

If a chained command has an error, execution will continue regardless.

**Notes:**  

 - `add`, `add_dir` and `rmv` actually modify the file, you may want to do a backup before.  
 - when adding songs, if the given path is relative it will be added with that path. If you want to add with absolute paths you must provide them explicitely.  
