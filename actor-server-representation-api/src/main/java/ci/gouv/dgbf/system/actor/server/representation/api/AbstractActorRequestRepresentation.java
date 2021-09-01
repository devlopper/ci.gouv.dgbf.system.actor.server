package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.representation.SpecificRepresentation;

import ci.gouv.dgbf.system.actor.server.representation.entities.AbstractActorRequestDto;

public interface AbstractActorRequestRepresentation<REQUEST extends AbstractActorRequestDto> extends SpecificRepresentation<REQUEST> {
	
	String PATH_RECORD = "record";
	@POST
	@Path(PATH_RECORD)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response record(@QueryParam(ActorRepresentation.PARAMETER_ACTORS) List<String> actorsIdentifiers
			,@QueryParam("requests") List<String> scopesIdentifiers
			,@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			,@QueryParam("ignoreexisting") Boolean ignoreExisting);
	
	String PATH_CANCEL = "cancel";
	@POST
	@Path(PATH_CANCEL)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response cancel(@QueryParam("identifiers") List<String> identifiers,@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode
			,@QueryParam("ignoreexisting") Boolean ignoreExisting);

	String PATH_PROCESS = "process";
	@POST
	@Path(PATH_PROCESS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({MediaType.APPLICATION_JSON})
	Response process(List<REQUEST> requestDtos,@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode);
	
	/**/
	
	String PARAMETER_PROCESSED = "traite";
	String DESCRIPTION_PROCESSED = "Traitement";
	String EXAMPLE_PROCESSED = "true";
	
	String PARAMETER_GRANTED = "accorde";
	String DESCRIPTION_GRANTED = "Accord";
	String EXAMPLE_GRANTED = "true";
}