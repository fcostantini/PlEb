# PlEb  
A simple tool to manage playlists (more info as development continues).  

## Requirements  

A modern version of stack. For Windows, Haskell Platform has everything you need.

## Installation  

    $ git clone https://github.com/fcostantini/PlEb.git  
    $ cd PlEb  
    $ stack setup  
    $ stack install  

It's recommended to add ~/.local/bin to your path for easier usage. You can also change the instalation directory with --local-bin-path dir.  

## Usage  

To load the playlist:  

    $ PlEb playlist-file  

### Operations  
add: adds a song to the playlist (currently generates a new file).  
print: shows contents of the playlist.  
export: copies every song to a single folder.  
