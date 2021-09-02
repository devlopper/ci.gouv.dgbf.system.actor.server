package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ScopeTypeRepresentation.PATH)
public interface ScopeTypeRepresentation extends RepresentationEntity<ScopeTypeDto> {
	
	String PATH = "scopetype";

	String TAG = ScopeRepresentation.TAG;

	String PARAMETER_REQUESTABLE = "demandable";
	String DESCRIPTION_REQUESTABLE = "Demandable";
	String EXAMPLE_REQUESTABLE = "true";
}