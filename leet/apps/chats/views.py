#import datetime
from chats.models import Chatroom
from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.contrib.auth import authenticate, login
from django.contrib.auth.decorators import login_required
from django.conf import settings
#from django.utils.dateformat import format
@login_required(login_url='/registration/accounts/login/')
def chat_home(request):

    return render_to_response("chat_home.html", {
        'user': request.user,
        'chatrooms': Chatroom.objects.all(),
        #'chat_server':settings.CHAT_SERVER_DOMAIN,
    },context_instance=RequestContext( request ))
    
@login_required(login_url='/registration/accounts/login/')
def chat_room_page(request, room_id):
    
    room=Chatroom.objects.get(id=room_id)
    #timestamp = format(datetime.now(), u'U')
    return render_to_response("chat_room_page.html", {
        'user': request.user,
        'room': room,
        
    },context_instance=RequestContext( request )) 
    
    
    
    
    