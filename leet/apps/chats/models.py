import datetime
from django.db import models
from djangotoolbox.fields import ListField, DictField, EmbeddedModelField
from django.conf import settings

class Chatroom(models.Model):
    
    #uuid = models.CharField(max_length=32)
    created = models.DateTimeField(default=datetime.datetime.utcnow)
    #messages = ListField(models.CharField(max_length=10240, null=True, blank=True))
    finished = models.BooleanField(default=False)
    title = models.CharField( max_length=255 )
    #objects = ChatroomManager()
    
    def get_absolute_url(self):
        return settings.CHAT_SERVER_DOMAIN + 'chat/room/%s' % self.title