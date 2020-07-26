package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestFunctionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AccountRequestFunctionRepresentation.PATH)
public interface AccountRequestFunctionRepresentation extends RepresentationEntity<AccountRequestFunctionDto> {
	
	String PATH = "accountrequestfunction";
	
}
