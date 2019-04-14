# Spotifyify

A command-line tool for importing your local music collection into Spotify.

Given the root directory of your music collection, Spotifyify will attempt to:

* Follow all the artists in your collection on Spotify
* Save all the albums in your collection to your Spotify library

## Commands

There are two commands, designed to be run in this order:

1. `build-manifest` - traverse a local directory to discover your music
   collection and write it to a file called a manifest.
2. `import` - given a manifest file, import your music collection into Spotify

## How to run

1. Clone this repo

2. [Install
   Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) (`brew
   install haskell-stack` on MacOS)

3. `stack build` to download the dependencies and build the tool (this might
   take a while)

4. Go to [the Spotify developer
   dashboard](https://developer.spotify.com/dashboard/applications) and create a
   new application

5. Click on "Edit settings" for your new app, and add the following redirect URI

    ```
    https://cb372.github.io/spotifyify/callback.html
    ```

6. Run the `build-manifest` command, telling it where to find your music collection

    ```
    stack run -- build-manifest ~/my-music-folder
    ```

    This will create a file called `manifest.yml`. You can open it in editor and
    sanity-check it if you like.

7. Passing your Spotify app's client ID and secret as environment variables, run
   the `import` command

   `SPOTIFY_CLIENT_ID=foo SPOTIFY_CLIENT_SECRET=bar stack run -- import`

   As part of the Spotify API authentication process, this will open a browser
   window. Copy the token from the browser window, paste it into stdin and press
   Enter.

   After you've authenticated, Spotifyify will import your music into Spotify,
   printing things to stdout as it goes.

   It will also log its progress to a file, with one JSON-encoded log entry per
   line.

## Checkpointing

When you start the `import` command, it reads both the manifest file and the log
file (if there is one), and filters out any artists from the manifest that have
already been processed.

That means you can safely kill the import process half way through. If you run
it again later, it will pick up where it left off and not duplicate any work.

## Rate limiting

If the import process results in too many calls to the Spotify API, API requests
will start getting rejected. The tool has support for this, and will back off
appropriately as soon as it encounters a 429 response.
