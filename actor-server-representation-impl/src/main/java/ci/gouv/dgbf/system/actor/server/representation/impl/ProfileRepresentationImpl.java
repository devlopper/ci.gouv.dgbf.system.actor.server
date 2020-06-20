package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ProfileRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ProfileRepresentationImpl extends AbstractRepresentationEntityImpl<ProfileDto> implements ProfileRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
