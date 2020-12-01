package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentificationFormAttributeDto;

@Path(IdentificationFormAttributeRepresentation.PATH)
public interface IdentificationFormAttributeRepresentation extends RepresentationEntity<IdentificationFormAttributeDto> {
	
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Enregistrer des attributs de formulaires")
	@APIResponses(value = {
			@APIResponse(responseCode = "200",name = "Enregistré")
	})
	Response save(Collection<IdentificationFormAttributeDto> formAttributeDtos);
	
	String PATH = "identificationformattribut";
	String PATH_SAVE = "save";
	
	static IdentificationFormAttributeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(IdentificationFormAttributeRepresentation.class);
	}
}