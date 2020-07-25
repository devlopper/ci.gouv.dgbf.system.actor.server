package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.BudgetaryFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(BudgetaryFunctionRepresentation.PATH)
public interface BudgetaryFunctionRepresentation extends RepresentationEntity<BudgetaryFunctionDto> {
	
	String PATH = "budgetaryfunction";
	
}
