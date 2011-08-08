""" Leet global views """

__author__ = "Eric Lee <eric.lee@geniecapital.com.tw>"


from django.shortcuts import render, render_to_response, redirect
from django.template.context import RequestContext


def home(request):
    """ Global homepage.  Redirects to user's profile page if authenticated, registration-signup
    otherwise. """

    return render_to_response('global_home.html', {'c':'this is context'}, context_instance=RequestContext( request ))