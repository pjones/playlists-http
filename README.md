playlists-http [![Travis][travis-shield]][travis]
==============

Download and parse playlists over HTTP.

## Usage

There are two interfaces, a simple `download` function that runs in
`IO` and a more complicated `download` function that uses `MonadIO`.

See the following modules for more details:

  * `Text.Playlist.HTTP.Simple`
  * `Text.Playlist.HTTP.Full`

There is also an `examples/example.hs` that demonstrates how to use
the `download` function which is found in `Text.Playlist.HTTP.Full`.

[travis]: https://travis-ci.org/pjones/playlists-http
[travis-shield]: https://travis-ci.org/pjones/playlists-http.svg?branch=master
