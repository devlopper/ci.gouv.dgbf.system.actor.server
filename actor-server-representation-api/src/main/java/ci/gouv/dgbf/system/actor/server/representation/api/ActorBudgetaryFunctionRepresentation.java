package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorBudgetaryFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActorBudgetaryFunctionRepresentation.PATH)
public interface ActorBudgetaryFunctionRepresentation extends RepresentationEntity<ActorBudgetaryFunctionDto> {
	
	String PATH = "actorbudgetaryfunction";
	
}
