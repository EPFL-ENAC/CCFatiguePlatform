FROM node:16 AS build-stage
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY ./ .
RUN npm run build

FROM nginxinc/nginx-unprivileged:1-alpine AS production-stage
USER root
RUN mkdir /app
COPY nginx.conf /etc/nginx/nginx.conf
COPY --from=build-stage /app/dist /app
RUN chown -R 101:0 /app
RUN chmod -R 755 /app
RUN chown 101:0 /etc/nginx/nginx.conf
USER 101
