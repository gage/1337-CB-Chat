from chats.models import Chatroom
from django.http import HttpResponse
import json

def create_chatroom(request):
    
    if request.is_ajax():
        if request.method == 'POST':
            
            title = request.POST.get('title', None)
            print 'fffffffffffffffffffffffffffff'
            chatroom = Chatroom.objects.create(title=title)
            
            data={'title':chatroom.title,
                  'url':chatroom.get_absolute_url(),
                  'id':chatroom.id}
            
    
            
            
        return HttpResponse(json.dumps({'status':1, 'data':data, 'msg':'success'}), mimetype="application/json")
    
    return HttpResponse(json.dumps({'status':0, 'msg':'failed'}), mimetype="application/json")