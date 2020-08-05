package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.BudgetSpecializationUnitDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(BudgetSpecializationUnitRepresentation.PATH)
public interface BudgetSpecializationUnitRepresentation extends RepresentationEntity<BudgetSpecializationUnitDto> {
	
	String PATH = "budgetspecializationunit";
	
}
