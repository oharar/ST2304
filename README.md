# ST2304

This is the material for ST2304 Statistisk modellering for biologer/bioteknologer (or ST2304 Statistical modelling for biologists/bioteknologists), which is run at NTNU. It includes a module for each week, and exercises starting at week 3. I have not included the solutions to the exercises, for obvious reasons.

Each module is written as an R markdown file. It includes explanations of the topic, in-class exercises (with hints and solutions) and links to videos and other material. In theory rendering the R markdown files will provide everytning, with the exception of embedded videos (see below).

## Videos

The videos are hosted on NTNU's Panopto site. If you need to render the R Markdown files to create a new html file, the videos don't embed properly. So instead you need to embed them by hand. There are two ways to do this, depending on what we've done:

The easiest is if we have already added the html code in the RMarkdown document. If we have done this it will be commented out, so try this:

- search the html file for "<!---". 
- check that this comments out a video: it should start with an iframe tag ("<iframe src=...."), and should be next to a link to the video
- remove the commenting out: "<!---" and "--->"
- save the file (and check it's OK)

If you do not find any videos this way, we probably added them with Plan B. So you need to do this:

- search the html file for "ADD EMBED". 
- work out which video goes with this. The link to the video should be just above the ADD EMBED.
- Go to the video, and getthe code: Settings (the cog at the top of the screen) > Share > Embed > Copy Embed Code
- Paste the code into the html file, in place of the "ADD EMBED X"

If that doesn't work, you could try to extract them from [previous years' pages](https://wiki.math.ntnu.no/st2304/2022v/start).

Note that you will have to do this evert time you re-make the html document.

## Acknowledgements

The material was initially created and revised by @oharar and @emilygsimmonds.
