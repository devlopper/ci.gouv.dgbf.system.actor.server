package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AccountRepresentation.PATH)
public interface AccountRepresentation extends RepresentationEntity<AccountDto> {
	
	String PATH = "account";
	
}
