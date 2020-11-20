package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestRepresentation.PATH)
public interface RequestRepresentation extends RepresentationEntity<RequestDto> {

	static RequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(RequestRepresentation.class);
	}
	
	String PATH = "demande";
	
	String TAG = "Demande";
}