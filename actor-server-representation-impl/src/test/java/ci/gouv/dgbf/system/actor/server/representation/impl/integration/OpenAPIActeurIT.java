package ci.gouv.dgbf.system.actor.server.representation.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;

import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.InSequence;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.junit.Test;

public class OpenAPIActeurIT extends AbstractIT {

    @Test @InSequence(1) @RunAsClient
    public void create(@ArquillianResource URL url) throws Exception {
    	String data = "nom=komenan&prenoms=yao&email=abc@mail.com";
    	URI uri = URI.create(url.toString()+"api/open/acteur/creer");
    	HttpRequest request = HttpRequest.newBuilder().uri(uri).header("Content-Type", "application/x-www-form-urlencoded")
    			.POST(BodyPublishers.ofString(data)).build();    	
    	HttpResponse<?> response = HttpClient.newHttpClient().send(request, BodyHandlers.discarding());
    	assertThat(response.statusCode()).isEqualTo(201);
    }

    @Test @InSequence(2) @RunAsClient
    public void create_required(@ArquillianResource URL url) throws Exception {
    	String data = "";
    	URI uri = URI.create(url.toString()+"api/open/acteur/creer");
    	HttpRequest request = HttpRequest.newBuilder().uri(uri).header("Content-Type", "application/x-www-form-urlencoded")
    			.POST(BodyPublishers.ofString(data)).build();    	
    	HttpResponse<?> response = HttpClient.newHttpClient().send(request, BodyHandlers.discarding());
    	assertThat(response.statusCode()).isEqualTo(400);
    }
    
    @Test @InSequence(3) @RunAsClient
    public void get(@ArquillianResource URL url) throws Exception {
    	URI uri = URI.create(url.toString()+"api/open/acteur/obtenir?nom_utilisateur=abc@mail.com");
    	HttpRequest request = HttpRequest.newBuilder().uri(uri).header("Content-Type", "application/json").GET().build();
    	HttpResponse<?> response = HttpClient.newHttpClient().send(request, BodyHandlers.discarding());
    	assertThat(response.statusCode()).isEqualTo(200);
    }
    
    @Test @InSequence(4) @RunAsClient
    public void get_required(@ArquillianResource URL url) throws Exception {
    	URI uri = URI.create(url.toString()+"api/open/acteur/obtenir");
    	HttpRequest request = HttpRequest.newBuilder().uri(uri).header("Content-Type", "application/json").GET().build();
    	HttpResponse<?> response = HttpClient.newHttpClient().send(request, BodyHandlers.discarding());
    	assertThat(response.statusCode()).isEqualTo(400);
    }
    
    @Test @InSequence(5) @RunAsClient
    public void get_notfound(@ArquillianResource URL url) throws Exception {
    	URI uri = URI.create(url.toString()+"api/open/acteur/obtenir?nom_utilisateur=xyz");
    	HttpRequest request = HttpRequest.newBuilder().uri(uri).header("Content-Type", "application/json").GET().build();
    	HttpResponse<?> response = HttpClient.newHttpClient().send(request, BodyHandlers.discarding());
    	assertThat(response.statusCode()).isEqualTo(404);
    }
    
    @Test @InSequence(6) @RunAsClient
    public void exists(@ArquillianResource URL url) throws Exception {
    	URI uri = URI.create(url.toString()+"api/open/acteur/exister?nom_utilisateur=abc@mail.com");
    	HttpRequest request = HttpRequest.newBuilder().uri(uri).header("Content-Type", "application/json").GET().build();
    	HttpResponse<?> response = HttpClient.newHttpClient().send(request, BodyHandlers.discarding());
    	assertThat(response.statusCode()).isEqualTo(200);
    }
}