package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ImputationDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ImputationRepresentation.PATH)
public interface ImputationRepresentation extends RepresentationEntity<ImputationDto> {
	
	String PATH = "imputation";
	
	String TAG = "Imputations";
}
