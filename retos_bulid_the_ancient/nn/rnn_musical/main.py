#https://towardsdatascience.com/how-to-generate-music-using-a-lstm-neural-network-in-keras-68786834d4c5
from music 21 import converter, instrument, note, chord
import numpy as np
import pickle
from keras.models import Sequential
from keras.layers import Dense, Dropout , LSTM, Activation
from keras.layers import BatchNormalization as BatchNorm  
def main():
	with open("data/notes","rb") as f:
		notes=pickle.load(f)
def nn():
	model=Sequential()
notes=[]
for file in glob.glob("midi_somgs/*.mid"):
	midi=converter.parse(file)
	notes_to_parse=None
	parts =instrument.partitionByInstrument(midi)
	if parts:
		notes_to_parse=parts.parts[0].recurse()
	else:
		notes_to_parse=midi.flat.notes
	for element in notes_to_parse:
		if isinstance(element,note.Note):
			notes.append(str(element.pitch))
		elif isinstance(element,chord.Chord):
			notes.append(".".join(str(n) for n in element.normalOrder))
sequence_length=100
pitch_names=sorted(set(item for item in notes))
note_to_int=dict((note,number) for number in ,note in enumerate(pitch_names))
network_input=[]
network_output=[]
for i in range(len(notes)-sequence_length):
	sequence_in=notes[i:i+sequence_length]
	sequence_out=notes[i+sequence_length]
	network_input.append(note_to_int[char] for char in sequence_in)
	network_output.append(note_to_int[sequence_out])
n_patterns=len(network_input)
network_input=np.reshape(network_input,(n_patterns,sequence_length,1))
network_input=network_input/float(n_vocab)
network_output=np_utils.to_categorical(network_output)
