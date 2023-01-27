#ifndef _TEXTEDITOR_FUNCS_H
#define _TEXTEDITOR_FUNCS_H
#include <string.h>
class texteditorfuncs
{
public:
	texteditorfuncs();
	string loadFile(string filename);
	void saveFile();
	void saveFileAs();
	~texteditorfuncs();
private:
	string filename;
};
#endif