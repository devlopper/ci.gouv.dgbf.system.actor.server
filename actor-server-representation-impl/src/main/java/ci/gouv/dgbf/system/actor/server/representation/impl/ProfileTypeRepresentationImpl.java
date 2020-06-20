package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ProfileTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileTypeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ProfileTypeRepresentationImpl extends AbstractRepresentationEntityImpl<ProfileTypeDto> implements ProfileTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
