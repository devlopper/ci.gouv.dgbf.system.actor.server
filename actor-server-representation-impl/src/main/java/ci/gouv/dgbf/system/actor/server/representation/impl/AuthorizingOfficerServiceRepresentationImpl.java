package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AuthorizingOfficerServiceRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AuthorizingOfficerServiceDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AuthorizingOfficerServiceRepresentationImpl extends AbstractRepresentationEntityImpl<AuthorizingOfficerServiceDto> implements AuthorizingOfficerServiceRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
