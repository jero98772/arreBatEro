FROM ubuntu

RUN apt update
RUN apt install python3-pip -y
RUN pip3 install flask

WORKDIR /docker_test

COPY . .

CMD ["python","-m","flask","run","--host=0.0.0.0"]