package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ProfileFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileFunctionDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ProfileFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ProfileFunctionDto> implements ProfileFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
