FROM ubuntu:18.04

ENV TZ=Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN mkdir -p /home/sip && \
    apt-get update -y && apt-get install -y unzip wget curl git libpython-dev python3 python3-pip python-pip ninja-build clang cmake radare2 libncurses5-dev && \
    apt-get upgrade -y --autoremove && \
    python3 -m pip install pip --upgrade && python3 -m pip install r2pipe matplotlib && \
    python -m pip install r2pipe pyelftools

WORKDIR /home/sip

# dynamoRIO is required for the tracer
RUN wget https://github.com/DynamoRIO/dynamorio/releases/download/release_8.0.0-1/DynamoRIO-Linux-8.0.0-1.tar.gz -O /home/sip/dynamorio.tar.gz && \
    cd /home/sip && tar xzf dynamorio.tar.gz && rm dynamorio.tar.gz

# dependencies for Triton
RUN apt-get update && apt-get install -y libcapstone-dev libboost1.65-dev
RUN git clone --depth 1 -b z3-4.8.7 https://github.com/Z3Prover/z3.git && \
    cd z3 && python scripts/mk_make.py && cd build && make -j4 && make install

# set up the rest of the repository
COPY . /home/sip
RUN apt-get install dos2unix && \
    find . -type f -exec dos2unix {} \; && \
    cd /home/sip/attacking_anti_tamper/tracer && ./build.sh Release && \
    cd /home/sip/attacking_anti_tamper/taint_cpp && ./build.sh Release && \
    cd /home/sip && mkdir built-sc-virt && cd built-sc-virt && \
    cmake ../sc-virt/src && \
    make

WORKDIR /home/sip

CMD ["/bin/bash"]
