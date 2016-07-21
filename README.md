# PlEb  
A simple tool to manage playlists. Supported formats are m3u, pls, wpl and xspf.  

## Requirements  

A modern version of stack. For Windows, Haskell Platform has everything you need.

## Installation  

    $ git clone https://github.com/fcostantini/PlEb.git  
    $ cd PlEb  
    $ stack setup  
    $ stack install  

It's recommended to add ~/.local/bin to your path for easier usage. You can also change the instalation directory with --local-bin-path dir.  

## Usage  

To load a playlist:  

    $ PlEb playlist-file  

### Operations  

**Note:** add and rmv actually modify the file, you may want to do a backup before.  

    add: adds a song to the playlist.  
    check: checks if the songs actually exist.  
    export: copies every (correct) song to a single folder.  
    load: loads another playlist.  
    print: shows contents of the playlist.  
    rmv: removes a song from the playlist.  
    exit/quit: exits the program.  
