# File Dump
Application to display the contents of a file in hexadecimal format.
This is similar to the macOS/Linux bash command hexdump, but you can scroll up and down through the file, and change bytes. It is also coloured for easier spotting of values.
You can also load in a second file to compare with the first, displaying the differences. Another feature is that you can XOR every byte in the entire file with a single value.
The output of the hex dump can be output to a text file.
The original two versions were written, on Windows, as I did not have anything suitable at the time. However, both these versions were 'rough and ready'. This version is compiled on Lazarus for Windows, macOS and Linux.
