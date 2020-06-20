package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.UserAccountProfileRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.UserAccountProfileDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class UserAccountProfileRepresentationImpl extends AbstractRepresentationEntityImpl<UserAccountProfileDto> implements UserAccountProfileRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
