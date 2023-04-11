from django.http import HttpResponse
def helloword(request):
	return HttpResponse("hola mundo")