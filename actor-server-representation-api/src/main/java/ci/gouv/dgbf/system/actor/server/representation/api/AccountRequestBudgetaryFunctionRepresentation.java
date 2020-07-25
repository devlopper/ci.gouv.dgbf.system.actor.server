package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestBudgetaryFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AccountRequestBudgetaryFunctionRepresentation.PATH)
public interface AccountRequestBudgetaryFunctionRepresentation extends RepresentationEntity<AccountRequestBudgetaryFunctionDto> {
	
	String PATH = "accountrequestbudgetaryfunction";
	
}
