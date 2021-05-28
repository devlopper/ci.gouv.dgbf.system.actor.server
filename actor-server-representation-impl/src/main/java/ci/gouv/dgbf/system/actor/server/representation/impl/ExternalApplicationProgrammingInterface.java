package ci.gouv.dgbf.system.actor.server.representation.impl;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.object.AbstractObject;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.servers.Server;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.business.api.ActorBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

@Path(ExternalApplicationProgrammingInterface.PATH)
@Server(url = "/api")
public class ExternalApplicationProgrammingInterface extends AbstractObject {

	public static final String PATH = "externe";
	
	/* Acteur */
	private static final String TAG_ACTOR = "Acteur";
	private static final String PATH_ACTOR = "acteur";
	private static final String PATH_ACTOR_CREATE = PATH_ACTOR+"/creer";
	
	@POST
	@Path(PATH_ACTOR_CREATE)
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Tag(name = TAG_ACTOR)
	@Operation(description = "Création acteur",operationId = "creer_acteur")
	@APIResponse(description = "Acteur créé",responseCode = "200", content = @Content(mediaType = MediaType.APPLICATION_JSON))
	public Response createActor(
			@Parameter(allowEmptyValue = false,description = "Nom",example = "Komenan",name = "nom",required = true)
			@FormParam("nom") String firstName
			
			,@Parameter(allowEmptyValue = false,description = "Prénoms",example = "Yao Chrstian",name = "prenoms",required = true)
			@FormParam("prenoms") String lastNames
			
			,@Parameter(description = "Email",example = "komenanyc@yahoo.fr",name = "email",required = true)
			@FormParam("email") String electronicMailAddress
			
			,@Parameter(description = "Identifiant de la civilité",example = "01",name = "civilite")
			@FormParam("civilite") String civilityIdentifier
			
			,@Parameter(description = "Identifiant du groupe",example = "02",name = "groupe")
			@FormParam("groupe") String groupIdentifier) {
		try {
			Actor actor = new Actor();
			actor.setFirstName(firstName).setLastNames(lastNames).setElectronicMailAddress(electronicMailAddress).setCivilityFromIdentifier(civilityIdentifier)
				.setGroupFromIdentifier(groupIdentifier);
			__inject__(ActorBusiness.class).create(actor);
		} catch (Exception exception) {
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(exception.toString()).build();
		}
		return Response.ok("Acteur créé.").build();
	}
}