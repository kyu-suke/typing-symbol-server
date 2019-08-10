# Use the existing Haskell image as our base
FROM haskell:8.2.2 as builder

 # Checkout our code onto the Docker container
WORKDIR /app
ADD . /app

 # Build and test our code, then install the “helloworld-haskell-exe” executable
RUN stack upgrade
RUN stack setup
RUN stack build --copy-bins

 # Copy the "helloworld-haskell-exe" executable to the image using docker multi stage build
FROM fpco/haskell-scratch:integer-gmp
WORKDIR /root/
COPY --from=builder /root/.local/bin/typing-symbol-server-exe .

 # Expose a port to run our application
EXPOSE 3000

 # Run the server command
CMD ["./typing-symbol-server-exe"]

