#https://towardsdatascience.com/how-to-generate-music-using-a-lstm-neural-network-in-keras-68786834d4c5
from music21 import converter, instrument, note, chord
import numpy as np
import pickle
from keras.models import Sequential
from keras.layers import Dense, Dropout , LSTM, Activation
from keras.layers import BatchNormalization as BatchNorm  
n_vocab=0;
def main():
	#notes=[]
	offsets=0
	step=0.5
	out_notes=[]
	with open("data/notes","rb") as f:
		notes=pickle.load(f)
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
	note_to_int=dict((note,number) for number,note in enumerate(pitch_names))
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
	start=np.random.randint(0,len(network_input)-1)
	int2note=dict((number,note) for number,note in enumerate(pitch_names))
	pattern=network_input[start]
	prediction_output=[]
	for notes in range(500):#number of notes
		prediction_input=np.reshape(pattern,(1,len(pattern),1))
		prediction_input=prediction_input/float(n_vocab)
		prediction=nn().predict(prediction_input,verbose=0)
		index=np.argmax(prediction)
		result=int2note[index]
		prediction_output.append(result)
		pattern.append(index)
		pattern=pattern[1:len(pattern)]
	for pattern in prediction_output:
		if("." in pattern) or pattern.isdigit():
			notes_in_chord=pattern.split(".")
			notes=[]
			for current_note in notes_in_chord:
				new_note = note.Note(int(current_note))
				new_note.storedInstrument = instrument.Piano()
				notes.append(new_note)
			new_chord = chord.Chord(notes)
			new_chord.offset = offset
			output_notes.append(new_chord)
		else:
			new_note = note.Note(pattern)
			new_note.offset = offset
			new_note.storedInstrument = instrument.Piano()
			output_notes.append(new_note)
		offset+=step
	midi_stream = stream.Stream(output_notes)
	midi_stream.write('midi', fp='test_output.mid')
def nn():
	model=Sequential()
	model.add(LSTM(256,input_shape=(network_input.shape[1], network_input.shape[2]),return_sequences=True))
	model.add(Dropout(0.3))
	model.add(LSTM(512,return_sequences=True))
	model.add(Dropout(0.3))
	model.add(LSTM(256))
	model.add(Dense(256))
	model.add(Dropout(0.3))
	model.add(Dense(n_vocab))
	model.add(Activation("softmax"))
	model.compile(loss="categorical_crossentropy",optimizer="rmsprop")
	model.load("weigths.hdf5")
	return model
