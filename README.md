# Time_limit

Version:
--------

0.9.4

In short:
---------

A windowed / full-screen countdown timer. Colour and font size changes are used as warnings. Progress-bar gives a glance at the time stream.

Useful for time-tracking  of your speech, lecture or presentation.

Colour / font / time limits can be changed using settings window and hot-keys. It's possible to save and restore several configurations. The input configuration file may be specified from command-line.

Available hot-keys:
-------------------

- [`Space`], [`s`], Mouse click – start/stop the timer.
- [`F11`], [`f`], Mouse double-click  – Full screen on/off.
- [`Esc`] – Exit.
- [`b`] – Border-less on/off.
- [`F1`], [`h`] – This help.
- [`m`], Mouse right click – Menu window.
- [`Left`], [`Up`] – Increase time by 1 minute.
- [`Right`], [`Down`] – Decrease time by 1 minute.
- [`r`], Mouse middle click - reset timer.
- [`c`] - Clock mode.
- [`e`] - Elapsed time mode.
- [`l`] - Remaining time mode.
- [`t`] - Toggles „Always on top” and normal windows modes

- You can change current time with the mouse wheel as well.
- By clicking on progress-bar, the timer mode is toggled.

Available command-line parameters:
----------------------------------

- `--config=`file.ini      Read the specified configuration file.
- `-s` or `--start`          Starts the countdown.
- `--run=`command          At the end of countdown launches another program.
- `-e` --exit              Exit at the end of countdown.
- `-l` nn or `--lang` nn     Set the interface language (nn is the language code).
- `-h` or `--help`           Show command-line help.

Availability:
-------------

The binary files are kept in Sourceforge:
https://sourceforge.net/projects/time-limit/

- License: GPLv3
- Programming Language: Lazarus, Free Pascal
