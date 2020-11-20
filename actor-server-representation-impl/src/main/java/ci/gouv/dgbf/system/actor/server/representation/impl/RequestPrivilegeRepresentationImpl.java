package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestPrivilegeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestPrivilegeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestPrivilegeRepresentationImpl extends AbstractRepresentationEntityImpl<RequestPrivilegeDto> implements RequestPrivilegeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
