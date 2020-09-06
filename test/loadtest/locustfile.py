from locust import task, between
from locust.contrib.fasthttp import FastHttpUser


class PostgRESTUser(FastHttpUser):
    wait_time = between(0.05, 0.2)

    @task(1)
    def index(self):
        self.client.get("/")

    @task(10)
    def articles(self):
        self.client.get("/articles")

    @task(10)
    def orders(self):
        self.client.get("/orders?select=name")
