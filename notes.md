
# Change Slides

Turn off all incremental lists.

# Download PNGs

For each slide (0-17), do this:

    python2.6 $(which webkit2png) --fullsize --filename=pythongis-NNN http://people.virginia.edu/~err8n/pythongis/#slideNNN

# Convert PNGs to PDF

Use ImageMagick:

    convert *.png pythongis.pdf

