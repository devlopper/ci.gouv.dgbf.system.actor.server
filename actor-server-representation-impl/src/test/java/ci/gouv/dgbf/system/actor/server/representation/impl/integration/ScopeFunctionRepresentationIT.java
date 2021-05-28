package ci.gouv.dgbf.system.actor.server.representation.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;

import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.InSequence;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.junit.Test;

import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionRepresentation;

public class ScopeFunctionRepresentationIT extends AbstractIT {

    @Test @InSequence(1)
    public void computeCreditManagerHolderNameByAdministrativeUnitIdentifier() {
    	assertThat(__inject__(ScopeFunctionRepresentation.class).computeCreditManagerHolderNameByAdministrativeUnitIdentifier("DTI").getEntity())
    		.isEqualTo("Gestionnaire de credits Direction des traitements informatiques");
    }
    
    @Test @InSequence(2)
    public void computeAuthorizingOfficerHolderNameByBudgetSpecializationUnitIdentifierByLocalityIdentifier() {
    	assertThat(__inject__(ScopeFunctionRepresentation.class).computeAuthorizingOfficerHolderNameByBudgetSpecializationUnitIdentifierByLocalityIdentifier(
    			"USB7d152f5a-3bcb-4ba3-a107-b680b6a230b3", null).getEntity())
    		.isEqualTo("Ordonnateur delegue du Programme Administration Générale du Sénat");
    	
    	assertThat(__inject__(ScopeFunctionRepresentation.class).computeAuthorizingOfficerHolderNameByBudgetSpecializationUnitIdentifierByLocalityIdentifier(
    			"USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2", null).getEntity())
    		.isEqualTo("Ordonnateur delegue du Programme Budget");
    	
    	assertThat(__inject__(ScopeFunctionRepresentation.class).computeAuthorizingOfficerHolderNameByBudgetSpecializationUnitIdentifierByLocalityIdentifier(
    			"USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2", "DIMBOKRO").getEntity())
    		.isEqualTo("Ordonnateur secondaire du Programme Budget a Dimbokro");
    }
    
    @Test @InSequence(3) @RunAsClient
    public void client(@ArquillianResource URL url) {
    	String uriString = url.toString()+"api/scopefunction/getByScopeIdentifierByFunctionIdentifier?domaine=DTI&fonction=GC";
    	HttpClient client = HttpClient.newHttpClient();
    	HttpRequest request = HttpRequest.newBuilder().uri(URI.create(uriString)).header("Content-Type", "application/json")
    			.GET().build();
    	client.sendAsync(request, BodyHandlers.ofString())
    	      .thenApply(HttpResponse::body)
    	      .thenAccept(System.out::println)
    	      .join();
    }
}