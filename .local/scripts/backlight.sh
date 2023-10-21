#!/bin/bash
# A script to control backlight brightness.
# Argument $1: either '-' for brightness up or '+' for brightness down

# Path to the sysfs file controlling backlight brightness
brightness_file="/sys/class/backlight/amdgpu_bl1/brightness"

# Step size for increasing/decreasing brightness.
step=5

case $1 in
  # Increase current brightness by `step` when `+` is passed to the script
  +) echo $(($(< "${brightness_file}") + ${step})) > "${brightness_file}";;

  # Decrease current brightness by `step` when `-` is passed to the script
  -) echo $(($(< "${brightness_file}") - ${step})) > "${brightness_file}";;
esac
