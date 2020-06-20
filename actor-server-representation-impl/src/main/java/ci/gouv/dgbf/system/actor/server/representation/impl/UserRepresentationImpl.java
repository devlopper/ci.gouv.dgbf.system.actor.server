package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.UserRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.UserDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class UserRepresentationImpl extends AbstractRepresentationEntityImpl<UserDto> implements UserRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
