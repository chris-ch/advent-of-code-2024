# Start with your desired image as the base
FROM ghcr.io/chris-ch/docker-ghcup:v1.0.9

# Add additional setup as needed
USER root

# Ensure proper permissions for the haskell user
RUN mkdir -p /home/haskell/.ssh && \
    chown -R haskell:haskell /home/haskell/.ssh

# Switch to the target user
USER haskell
