package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AccountRequestRepresentation.PATH)
public interface AccountRequestRepresentation extends RepresentationEntity<AccountRequestDto> {
	
	String PATH = "accountrequest";
	
}
