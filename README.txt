An LCD controller internal clock frequency tester for the TI-84 Plus CE and TI-83 Premium CE family.

Builds with SPASM-ng. A pre-built program is included in the repo for convenience.

Start the program and a moving pattern should appear on the screen.
If the image is scrolling left, press or hold the right arrow to adjust the LCD timings to slow it down.
Similarly, if the image is scrolling right, press or hold the left arrow.
The angle or shade of the pattern of the screen changes based on the automatically generated timing, which is normal.
Epilepsy warning: Intentionally increasing the speed of the pattern may cause rapidly flashing images.

Continue until the image is moving as slowly as possible.
Since the LCD controller clock frequency is not fully stable, it may be worth watching for a short while: if the image randomly changes between moving left or right, that's the closest possible timing.
Press CLEAR to exit, and an approximation of the LCD controller's internal frequency in Hz will be printed.