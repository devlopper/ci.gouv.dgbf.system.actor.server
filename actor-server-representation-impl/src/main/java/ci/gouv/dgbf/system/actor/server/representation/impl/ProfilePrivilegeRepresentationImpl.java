package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ProfilePrivilegeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfilePrivilegeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ProfilePrivilegeRepresentationImpl extends AbstractRepresentationEntityImpl<ProfilePrivilegeDto> implements ProfilePrivilegeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
