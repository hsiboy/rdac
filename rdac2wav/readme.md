Example usage:

`mtp2wav c:\songs\song0003\take01df.vr6 guitar.wav`

It currently supports 44.1K and 48K sample rate and 16-bit output.

If you have a 48000KHz MTP, you can now generate a WAV at the correct rate. For example to generate a 16-bit 48000KHz WAV:
` C:> mtp2wav -r 48000 -d 16 g:\song0003.vr6\take01df.vr6 guitar.wav`
