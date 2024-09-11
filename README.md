# ELEC391--Digital-Communication-System
## Worked in a team of 3 to design a digital communication system for audio and data communication under design constraints.
* 4 KHz Bandwidth
* 10<sup>-5</sup> target Bit Error Rate
* 100 KHz Spectral Mask
* 25 ms Delay
The system includes following modules:
* Source/Sink
* ADC/DAC
* Modulator/Demodulator
* Error Encoding/Decoding
* Transmission/Receiver
* Gilbert Fading Channel
The system was developed and tested using Simulink and Modelsim, and was prototyped on the Altera DE1-SoC FPGA board. Our design focused on achieving high fidelity and low bit error rate (BER) while operating within the specified bandwidth and processing delay constraints. All our subsystems and the overall design are functioning correctly and completely in both simulink and on the FPGA. We tested with a 48 KHz audio file as well as microphone inputs.

