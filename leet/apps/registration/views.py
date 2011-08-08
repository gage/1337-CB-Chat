from django import forms
from django.contrib.auth.forms import UserCreationForm
from django.http import HttpResponseRedirect
from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.contrib.auth import authenticate, login

def signup(request):
    if request.method == 'POST':
        form = UserCreationForm(request.POST)
        if form.is_valid():
            new_user = form.save()
            
            #user = authenticate()
            #login(request, user)
            
            return HttpResponseRedirect("/registration/accounts/login/")
    else:
        form = UserCreationForm()
        
    return render_to_response("signup.html", {
        'form': form,
    },context_instance=RequestContext( request ))
