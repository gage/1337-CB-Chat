from chats.models import Chatroom
from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.contrib.auth import authenticate, login
from django.contrib.auth.decorators import login_required
from django.conf import settings

@login_required(login_url='/registration/accounts/login/')
def chat_home(request):

    return render_to_response("chat_home.html", {
        'user': request.user,
        'chatrooms': Chatroom.objects.all(),
        'chat_server':settings.CHAT_SERVER_DOMAIN,
    },context_instance=RequestContext( request ))